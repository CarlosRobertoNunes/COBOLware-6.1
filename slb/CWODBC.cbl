      $Set SQL(DBMan=ODBC) SQL(ANSI92Entry)
      $SET RTNCODE-SIZE(2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWODBC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  07/05/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Conecta a DataSource ODBC                    *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 SQLMSG      PIC X(50) VALUE SPACES.
           05 SQLCODE-ed  PIC Z(9)- VALUE 0.

       COPY CWSEND.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC
       01  ODBCSTRING               PIC X(1024).
       EXEC SQL    END DECLARE SECTION END-EXEC
       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.

       01  LOGIN PIC X(8).

       PROCEDURE DIVISION USING LOGIN.

           EVALUATE LOGIN
               WHEN 'COMMIT'
                    EXEC SQL COMMIT TRANSACTION END-EXEC
                    GOBACK
               WHEN 'ROLLBACK'
                    EXEC SQL ROLLBACK TRANSACTION END-EXEC
                    GOBACK
               WHEN 'ON'
                    MOVE SPACE TO ODBCSTRING SQLMSG
                    DISPLAY "CWODBCSTRING" UPON ENVIRONMENT-NAME
                    ACCEPT   ODBCSTRING        FROM ENVIRONMENT-VALUE
                    DISPLAY 'SQLMSG' UPON ENVIRONMENT-NAME
                    ACCEPT SQLMSG FROM ENVIRONMENT-VALUE
               WHEN OTHER
                    GOBACK
           END-EVALUATE.

           EXEC SQL
                CONNECT TO :ODBCSTRING
           END-EXEC

           IF   SQLCODE NOT = 0
                MOVE SPACES to cwsend-msg
                MOVE SQLCODE TO SQLCODE-ED
                INSPECT SQLERRMC
                        CONVERTING ACENTOS-WINDOWS
                         TO ACENTOS-850
                STRING SQLCODE-ED ' '
                  SQLERRMC (1: SQLERRML - 1) DELIMITED BY SIZE
                  INTO CWSEND-MSG
                CALL 'CWPACK' USING CWSEND-MSG LENGTH CWSEND-MSG
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
                MOVE SQLCODE TO RETURN-CODE
                STOP RUN
           END-IF

           GOBACK.

       END PROGRAM CWODBC.
