      $Set NoMs
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
       01  SQLFPN GLOBAL.
           02  SQLFPN-FILE-LEN PIC S9(4) COMP-5 VALUE +25.
           02  SQLFPN-FILENAME PIC X(25) VALUE "G:\cobware\slb\CWORCN.pc
      -    "o".

       01  SQLCTX GLOBAL PIC S9(9) COMP-5 VALUE +1401297371.


       01  SQLEXD GLOBAL.
           02  SQL-SQLVSN   PIC S9(9) COMP-5 VALUE +10.
           02  SQL-ARRSIZ   PIC S9(9) COMP-5 VALUE +4.
           02  SQL-ITERS    PIC S9(9) COMP-5.
           02  SQL-OFFSET   PIC S9(9) COMP-5.
           02  SQL-SELERR   PIC S9(4) COMP-5.
           02  SQL-SQLETY   PIC S9(4) COMP-5.
           02  SQL-OCCURS   PIC S9(9) COMP-5.
           02  SQL-CUD      PIC S9(9) COMP-5.
           02  SQL-SQLEST   PIC S9(9) COMP-5.
           02  SQL-STMT     PIC S9(9) COMP-5.
           02  SQL-SQLADTP  PIC S9(9) COMP-5 VALUE 0.
           02  SQL-SQLTDSP  PIC S9(9) COMP-5 VALUE 0.
           02  SQL-SQPHSV   PIC S9(9) COMP-5.
           02  SQL-SQPHSL   PIC S9(9) COMP-5.
           02  SQL-SQPHSS   PIC S9(9) COMP-5.
           02  SQL-SQPIND   PIC S9(9) COMP-5.
           02  SQL-SQPINS   PIC S9(9) COMP-5.
           02  SQL-SQPARM   PIC S9(9) COMP-5.
           02  SQL-SQPARC   PIC S9(9) COMP-5.
           02  SQL-SQPADTO  PIC S9(9) COMP-5.
           02  SQL-SQPTDSO  PIC S9(9) COMP-5.
           02  SQL-SQHSTV   PIC S9(9) COMP-5 OCCURS 4 TIMES.
           02  SQL-SQHSTL   PIC S9(9) COMP-5 OCCURS 4 TIMES.
           02  SQL-SQHSTS   PIC S9(9) COMP-5 OCCURS 4 TIMES.
           02  SQL-SQINDV   PIC S9(9) COMP-5 OCCURS 4 TIMES.
           02  SQL-SQINDS   PIC S9(9) COMP-5 OCCURS 4 TIMES.
           02  SQL-SQHARM   PIC S9(9) COMP-5 OCCURS 4 TIMES.
           02  SQL-SQHARC   PIC S9(9) COMP-5 OCCURS 4 TIMES.
           02  SQL-SQADTO   PIC S9(4) COMP-5 OCCURS 4 TIMES.
           02  SQL-SQTDSO   PIC S9(4) COMP-5 OCCURS 4 TIMES.


       01  SQL-RUNTIME-VARS.
           02  SQL-IAPXIT-SUCCESS  PIC S9(9) COMP-5 VALUE    +0.
           02  SQL-IAPXIT-FAILURE  PIC S9(9) COMP-5 VALUE +1403.
           02  SQL-IAPXIT-FATALERR PIC S9(9) COMP-5 VALUE  +535.

       01  SQLCUD GLOBAL.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +4130.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +29.
           02     FILLER PIC S9(4) COMP-5 VALUE +76.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +20.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +31.
           02     FILLER PIC S9(4) COMP-5 VALUE +86.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +35.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +30.
           02     FILLER PIC S9(4) COMP-5 VALUE +101.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +50.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +27.
           02     FILLER PIC S9(4) COMP-5 VALUE +170.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +81.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +27.
           02     FILLER PIC S9(4) COMP-5 VALUE +175.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +112.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +27.
           02     FILLER PIC S9(4) COMP-5 VALUE +194.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +143.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +27.
           02     FILLER PIC S9(4) COMP-5 VALUE +198.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.

       01  AREAS-DE-TRABALHO.
           05 CWMENU      PIC X     VALUE SPACES.
           05 RECO        PIC X(3)  VALUE SPACES.
           05 SHOWCONNECT PIC X(3)  VALUE SPACES.
           05 SQLMSG      PIC X(50) VALUE SPACES.
           05 SQLCODE-ed  PIC Z(9)- VALUE 0.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 MSG                      PIC  X(068) VALUE SPACES.
      *    05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
      *    05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
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

      *    EXEC SQL BEGIN DECLARE SECTION END-EXEC.
      *01  USERNAME          PIC X(10) VARYING.
       01  USERNAME.
           02  USERNAME-LEN    PIC S9(4) COMP-5.
           02  USERNAME-ARR    PIC X(10).
      *01  PASSWD            PIC X(10) VARYING.
       01  PASSWD.
           02  PASSWD-LEN    PIC S9(4) COMP-5.
           02  PASSWD-ARR    PIC X(10).
       01  DATABASE          PIC X(20) VALUE SPACES.
      *01  LOGIN             PIC X(70) VARYING.
       01  LOGIN.
           02  LOGIN-LEN    PIC S9(4) COMP-5.
           02  LOGIN-ARR    PIC X(70).
      *    EXEC SQL END DECLARE SECTION END-EXEC.
      *    EXEC SQL INCLUDE SQLCA END-EXEC.
       01  SQLCA GLOBAL.
           05  SQLCAID               PIC X(8).
           05  SQLCABC               PIC S9(9) COMP-5.
           05  SQLCODE               PIC S9(9) COMP-5.
           05  SQLERRM.
               49 SQLERRML           PIC S9(4) COMP-5.
               49 SQLERRMC           PIC X(70).
           05  SQLERRP               PIC X(8).
           05  SQLERRD OCCURS 6 TIMES
                                     PIC S9(9) COMP-5.
           05  SQLWARN.
               10 SQLWARN0           PIC X(1).
               10 SQLWARN1           PIC X(1).
               10 SQLWARN2           PIC X(1).
               10 SQLWARN3           PIC X(1).
               10 SQLWARN4           PIC X(1).
               10 SQLWARN5           PIC X(1).
               10 SQLWARN6           PIC X(1).
               10 SQLWARN7           PIC X(1).
           05  SQLEXT                PIC X(8).

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
      *       EXEC SQL
      *            COMMIT
      *       END-EXEC
              MOVE 1 TO SQL-ITERS
              MOVE 5 TO SQL-OFFSET
              MOVE 0 TO SQL-OCCURS
              CALL "SQLADR" USING
                  SQLCUD
                  SQL-CUD
              CALL "SQLADR" USING
                  SQLCA
                  SQL-SQLEST
              MOVE 256 TO SQL-SQLETY

              CALL "SQLBEX" USING
                  SQLCTX
                  SQLEXD
                  SQLFPN
              IF RECO = 'ON'
                 GO TO RETRY
              END-IF
              GOBACK
           END-IF

           IF ORACLE-LOGIN (1:8)  = 'ROLLBACK'
      *       EXEC SQL
      *            ROLLBACK
      *       END-EXEC
              MOVE 1 TO SQL-ITERS
              MOVE 20 TO SQL-OFFSET
              MOVE 0 TO SQL-OCCURS
              CALL "SQLADR" USING
                  SQLCUD
                  SQL-CUD
              CALL "SQLADR" USING
                  SQLCA
                  SQL-SQLEST
              MOVE 256 TO SQL-SQLETY

              CALL "SQLBEX" USING
                  SQLCTX
                  SQLEXD
                  SQLFPN
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
121014          goback
                MOVE 2 TO ONOFF
                IF CWUNIX-OS-CODE NUMERIC display MSG AT 2501 END-IF
      *         EXEC SQL COMMIT RELEASE END-EXEC
                MOVE 1 TO SQL-ITERS
                MOVE 35 TO SQL-OFFSET
                MOVE 0 TO SQL-OCCURS
                CALL "SQLADR" USING
                    SQLCUD
                    SQL-CUD
                CALL "SQLADR" USING
                    SQLCA
                    SQL-SQLEST
                MOVE 256 TO SQL-SQLETY

                CALL "SQLBEX" USING
                    SQLCTX
                    SQLEXD
                    SQLFPN
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
      *            EXEC SQL
      *                 CONNECT :USERNAME IDENTIFIED BY :PASSWD
      *                   USING :DATABASE
      *            END-EXEC
                   CALL "ORASQL8"
                   MOVE 10 TO SQL-ITERS
                   MOVE 50 TO SQL-OFFSET
                   MOVE 0 TO SQL-OCCURS
                   CALL "SQLADR" USING
                       SQLCUD
                       SQL-CUD
                   CALL "SQLADR" USING
                       SQLCA
                       SQL-SQLEST
                   MOVE 256 TO SQL-SQLETY
                   CALL "SQLADR" USING
                       USERNAME-LEN IN
                       USERNAME
                       SQL-SQHSTV(1)
                   MOVE 12 TO SQL-SQHSTL(1)
                   MOVE 0 TO SQL-SQHSTS(1)
                   MOVE 0 TO SQL-SQINDV(1)
                   MOVE 0 TO SQL-SQHARM(1)
                   CALL "SQLADR" USING
                       PASSWD-LEN IN
                       PASSWD
                       SQL-SQHSTV(2)
                   MOVE 12 TO SQL-SQHSTL(2)
                   MOVE 0 TO SQL-SQHSTS(2)
                   MOVE 0 TO SQL-SQINDV(2)
                   MOVE 0 TO SQL-SQHARM(2)
                   CALL "SQLADR" USING
                       DATABASE
                       SQL-SQHSTV(3)
                   MOVE 20 TO SQL-SQHSTL(3)
                   MOVE 0 TO SQL-SQHSTS(3)
                   MOVE 0 TO SQL-SQINDV(3)
                   MOVE 0 TO SQL-SQHARM(3)
                   CALL "SQLADR" USING
                       SQL-SQHSTV(1)
                       SQL-SQPHSV
                   CALL "SQLADR" USING
                       SQL-SQHSTL(1)
                       SQL-SQPHSL
                   CALL "SQLADR" USING
                       SQL-SQHSTS(1)
                       SQL-SQPHSS
                   CALL "SQLADR" USING
                       SQL-SQINDV(1)
                       SQL-SQPIND
                   CALL "SQLADR" USING
                       SQL-SQINDS(1)
                       SQL-SQPINS
                   CALL "SQLADR" USING
                       SQL-SQHARM(1)
                       SQL-SQPARM
                   CALL "SQLADR" USING
                       SQL-SQHARC(1)
                       SQL-SQPARC

                   CALL "SQLBEX" USING
                       SQLCTX
                       SQLEXD
                       SQLFPN
                ELSE
      *            EXEC SQL
      *                 CONNECT :LOGIN
      *                   USING :DATABASE
      *            END-EXEC
                   CALL "ORASQL8"
                   MOVE 10 TO SQL-ITERS
                   MOVE 81 TO SQL-OFFSET
                   MOVE 0 TO SQL-OCCURS
                   CALL "SQLADR" USING
                       SQLCUD
                       SQL-CUD
                   CALL "SQLADR" USING
                       SQLCA
                       SQL-SQLEST
                   MOVE 256 TO SQL-SQLETY
                   CALL "SQLADR" USING
                       LOGIN-LEN IN
                       LOGIN
                       SQL-SQHSTV(1)
                   MOVE 72 TO SQL-SQHSTL(1)
                   MOVE 0 TO SQL-SQHSTS(1)
                   MOVE 0 TO SQL-SQINDV(1)
                   MOVE 0 TO SQL-SQHARM(1)
                   CALL "SQLADR" USING
                       DATABASE
                       SQL-SQHSTV(3)
                   MOVE 20 TO SQL-SQHSTL(3)
                   MOVE 0 TO SQL-SQHSTS(3)
                   MOVE 0 TO SQL-SQINDV(3)
                   MOVE 0 TO SQL-SQHARM(3)
                   CALL "SQLADR" USING
                       SQL-SQHSTV(1)
                       SQL-SQPHSV
                   CALL "SQLADR" USING
                       SQL-SQHSTL(1)
                       SQL-SQPHSL
                   CALL "SQLADR" USING
                       SQL-SQHSTS(1)
                       SQL-SQPHSS
                   CALL "SQLADR" USING
                       SQL-SQINDV(1)
                       SQL-SQPIND
                   CALL "SQLADR" USING
                       SQL-SQINDS(1)
                       SQL-SQPINS
                   CALL "SQLADR" USING
                       SQL-SQHARM(1)
                       SQL-SQPARM
                   CALL "SQLADR" USING
                       SQL-SQHARC(1)
                       SQL-SQPARC

                   CALL "SQLBEX" USING
                       SQLCTX
                       SQLEXD
                       SQLFPN
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
      *             EXEC SQL
      *                  CONNECT :USERNAME IDENTIFIED BY :PASSWD
      *             END-EXEC
            CALL "ORASQL8"
            MOVE 10 TO SQL-ITERS
            MOVE 112 TO SQL-OFFSET
            MOVE 0 TO SQL-OCCURS
            CALL "SQLADR" USING
                SQLCUD
                SQL-CUD
            CALL "SQLADR" USING
                SQLCA
                SQL-SQLEST
            MOVE 256 TO SQL-SQLETY
            CALL "SQLADR" USING
                USERNAME-LEN IN
                USERNAME
                SQL-SQHSTV(1)
            MOVE 12 TO SQL-SQHSTL(1)
            MOVE 0 TO SQL-SQHSTS(1)
            MOVE 0 TO SQL-SQINDV(1)
            MOVE 0 TO SQL-SQHARM(1)
            CALL "SQLADR" USING
                PASSWD-LEN IN
                PASSWD
                SQL-SQHSTV(2)
            MOVE 12 TO SQL-SQHSTL(2)
            MOVE 0 TO SQL-SQHSTS(2)
            MOVE 0 TO SQL-SQINDV(2)
            MOVE 0 TO SQL-SQHARM(2)
            CALL "SQLADR" USING
                SQL-SQHSTV(1)
                SQL-SQPHSV
            CALL "SQLADR" USING
                SQL-SQHSTL(1)
                SQL-SQPHSL
            CALL "SQLADR" USING
                SQL-SQHSTS(1)
                SQL-SQPHSS
            CALL "SQLADR" USING
                SQL-SQINDV(1)
                SQL-SQPIND
            CALL "SQLADR" USING
                SQL-SQINDS(1)
                SQL-SQPINS
            CALL "SQLADR" USING
                SQL-SQHARM(1)
                SQL-SQPARM
            CALL "SQLADR" USING
                SQL-SQHARC(1)
                SQL-SQPARC

            CALL "SQLBEX" USING
                SQLCTX
                SQLEXD
                SQLFPN
                 ELSE
      *             EXEC SQL
      *                  CONNECT :LOGIN
      *             END-EXEC
            CALL "ORASQL8"
            MOVE 10 TO SQL-ITERS
            MOVE 143 TO SQL-OFFSET
            MOVE 0 TO SQL-OCCURS
            CALL "SQLADR" USING
                SQLCUD
                SQL-CUD
            CALL "SQLADR" USING
                SQLCA
                SQL-SQLEST
            MOVE 256 TO SQL-SQLETY
            CALL "SQLADR" USING
                LOGIN-LEN IN
                LOGIN
                SQL-SQHSTV(1)
            MOVE 72 TO SQL-SQHSTL(1)
            MOVE 0 TO SQL-SQHSTS(1)
            MOVE 0 TO SQL-SQINDV(1)
            MOVE 0 TO SQL-SQHARM(1)
            CALL "SQLADR" USING
                SQL-SQHSTV(1)
                SQL-SQPHSV
            CALL "SQLADR" USING
                SQL-SQHSTL(1)
                SQL-SQPHSL
            CALL "SQLADR" USING
                SQL-SQHSTS(1)
                SQL-SQPHSS
            CALL "SQLADR" USING
                SQL-SQINDV(1)
                SQL-SQPIND
            CALL "SQLADR" USING
                SQL-SQINDS(1)
                SQL-SQPINS
            CALL "SQLADR" USING
                SQL-SQHARM(1)
                SQL-SQPARM
            CALL "SQLADR" USING
                SQL-SQHARC(1)
                SQL-SQPARC

            CALL "SQLBEX" USING
                SQLCTX
                SQLEXD
                SQLFPN
                 END-IF
                 MOVE SPACES TO MSG
                 STRING "Desconectando "  DELIMITED BY SIZE
                        USERNAME-ARR   DELIMITED BY SPACE
                        " do banco..." DELIMITED BY SIZE
                   INTO MSG
           END-IF

           IF   SQLCODE NOT = 0
                MOVE SQLERRMC (1: SQLERRML - 1) TO CWSEND-MSG
                MOVE SPACES  to cwsend-msg
                MOVE SQLCODE TO SQLCODE-ED
                INSPECT SQLERRMC
                        CONVERTING ACENTOS-WINDOWS
                         TO ACENTOS-850
                STRING SQLCODE-ED ' '
                  SQLERRMC (1: SQLERRML - 1) DELIMITED BY SIZE
                  INTO CWSEND-MSG
                DISPLAY 'SQLERRMC' UPON ENVIRONMENT-NAME
                DISPLAY CWSEND-MSG UPON ENVIRONMENT-VALUE
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
                IF   SQLCODE = -1034
                OR   SQLCODE = -1033
                     GO TO RETRY
                ELSE
                     IF   SQLCODE NOT = 0
                          MOVE SQLCODE TO RETURN-CODE
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
